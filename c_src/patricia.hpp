#include <algorithm>
#include <cstdint>
#include <vector>
#include <iostream>
#include <set>
using namespace std;

template <typename KeyType>
class PatriciaKey {
public:
    PatriciaKey(KeyType value, uint8_t offset) : value_(value), offset_(offset)
    {
        if (offset == (sizeof(KeyType) << 3))
            value_ = 0;
        else
            value_ &= static_cast<KeyType>(0xffffffffffffffffll << offset_);
    }

    KeyType value() const {
        return value_;
    }

    uint8_t offset() const {
        return offset_;
    }

    bool operator==(const PatriciaKey<KeyType>& key) const {
        return value_ == key.value_ && offset_ == key.offset_;
    }

    bool operator<(const PatriciaKey<KeyType>& key) const {
        if (value_ < key.value_)
            return true;
        else if (value_ == key.value_)
            return offset_ > key.offset_;
        else
            return false;
    }

    bool applies_to(KeyType key) const {
        if (offset_ == (sizeof(KeyType) << 3))
            return true;
        return (key & static_cast<KeyType>(0xffffffffffffffffll << offset_)) == value_;
    }

    bool applies_to(const PatriciaKey<KeyType> patkey) const {
        if (offset_ == (sizeof(KeyType) << 3))
            return true;
        return offset_ >= patkey.offset_ &&
            (patkey.value() & static_cast<KeyType>(0xffffffffffffffffll << offset_)) == value_;
    }

private:

    KeyType value_;
    uint8_t offset_;
};

template <typename KeyType, typename ValueType>
class PatriciaPair {

public:

    PatriciaPair(std::vector<PatriciaKey<KeyType>>& keys, ValueType value)
    {
        keys_ = keys;
        value_ = value;
    }

    ValueType value() const {
        return value_;
    }

    unsigned get_size() {
        return keys_.size();
    }

    PatriciaKey<KeyType> get_key(unsigned i) {
        return keys_[i];
    }

private:
    std::vector<PatriciaKey<KeyType> > keys_;
    ValueType value_;

};

template <typename KeyType, typename ValueType>
class PatriciaNode
{
    typedef std::pair<PatriciaKey<KeyType>, PatriciaNode *> Child;
    typedef std::pair<PatriciaKey<KeyType>, ValueType> InsertedChild;

public:
    PatriciaNode() {}

    PatriciaNode(std::set<ValueType>& parent_set, ValueType& value)
    {
        creation_set = parent_set;
        creation_set.insert(value);
    }

    ~PatriciaNode()
    {
        unsigned size = children.size();
        for (unsigned i = 0; i < size; i++)
        {
            delete children[i].second;
        }
    }

    void
    insert(PatriciaKey<KeyType>& new_key, ValueType& new_value)
    {
        int min = 0;
        int max = children.size()-1;

        while (max >= min)
        {
            int mid = min + ((max - min) / 2);
            PatriciaKey<KeyType> current_key = children[mid].first;
            if (current_key == new_key)
            {
                children[mid].second->add(new_value);
                return;
            }
            else if (current_key.applies_to(new_key))
            {
                children[mid].second->insert(new_key, new_value);
                return;
            }
            // new_key should never apply to current_key if insertion order is respected
            else if(new_key.value() < current_key.value())
                max = mid - 1;
            else
                min = mid + 1;
        }

        inserted_children.push_back(InsertedChild(new_key, new_value));
    }

    void
    add(ValueType new_value)
    {
        creation_set.insert(new_value);
    }

    void finalize_offset()
    {
        for (auto child : children)
             child.second->finalize_offset();

        if (inserted_children.size() > 0) {
            PatriciaKey<KeyType> current_key = inserted_children[0].first;
            PatriciaNode<KeyType, ValueType> *current_node = new PatriciaNode<KeyType, ValueType>(creation_set, inserted_children[0].second);
            for (unsigned i = 1; i < inserted_children.size(); i++) {
                if (inserted_children[i].first == current_key)
                    current_node->add(inserted_children[i].second);
                else {
                    children.push_back(Child(current_key, current_node));
                    current_key =  inserted_children[i].first;
                    current_node = new PatriciaNode<KeyType, ValueType>(creation_set, inserted_children[i].second);
                }
            }
            children.push_back(Child(current_key, current_node));
        }

        inserted_children.clear();
        std::sort(children.begin(), children.end());
    }

    void
    finalize()
    {
        values.reserve(creation_set.size());
        values.insert(values.begin(), creation_set.begin(), creation_set.end());
        creation_set.clear();
        for (auto child : children)
            child.second->finalize();
    }

    std::vector<ValueType> *
    lookup(KeyType key)
    {

        int min = 0;
        int max = children.size()-1;

        while (max >= min)
        {
            int mid = min + ((max - min) / 2);
            PatriciaKey<KeyType> current_key = children[mid].first;
            if (current_key.applies_to(key))
            {
                return children[mid].second->lookup(key);
            }
            else if(key < current_key.value())
                max = mid - 1;
            else
                min = mid + 1;
        }

        return &values;
    }

private:
    std::set<ValueType> creation_set;
    std::vector<ValueType> values;

    std::vector<Child> children;

    // This is to optimize insertion of new nodes
    std::vector<InsertedChild> inserted_children;
};

template <typename KeyType, typename ValueType>
class Patricia {
public:

    Patricia(std::vector<PatriciaPair<KeyType, ValueType> >& pairs)
    {
        unsigned length = pairs.size();
        for (int offset = sizeof(KeyType) << 3; offset >= 0; offset--)
        {
            for (unsigned i = 0; i < length; i++)
            {
                ValueType value = pairs[i].value();
                unsigned keys_length = pairs[i].get_size();
                for (unsigned j = 0; j < keys_length; j++)
                {
                    if (pairs[i].get_key(j).offset() == offset)
                    {
                        //cout << offset << endl;
                        PatriciaKey<KeyType> current_key = pairs[i].get_key(j);
                        root.insert(current_key, value);
                    }
                }
            }
            root.finalize_offset();
        }
        root.finalize();
    }

    std::vector<ValueType> *
    lookup (KeyType key)
    {
        return root.lookup(key);
    }

private:
    PatriciaNode<KeyType, ValueType> root;

};
