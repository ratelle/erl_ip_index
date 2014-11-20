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
struct PatriciaElem {
    uint8_t offset;
    KeyType key;
    ValueType value;

    PatriciaElem (uint8_t offset_, KeyType key_, ValueType value_) {
        offset = offset_;
        key = key_;
        value = value_;
    }

    bool operator< (const PatriciaElem& val) const {
        if (offset > val.offset) {
            return true;
        }
        else if (offset == val.offset) {
            if (key < val.key)
                return true;
            else if (key == val.key)
                return value < val.value;
        }

        return false;
    }

    bool operator== (const PatriciaElem& val) const {
        return offset == val.offset && key == val.key && value == val.value;
    }

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

    PatriciaNode(std::vector<ValueType>& parent_values, ValueType value)
    {
        values = parent_values;
        values.push_back(value);
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
    insert(PatriciaKey<KeyType>& new_key, ValueType new_value)
    {
        int min = 0;
        int max = children.size()-1;

        while (max >= min)
        {
            int mid = min + ((max - min) / 2);
            PatriciaKey<KeyType> current_key = children[mid].first;
            if (current_key.applies_to(new_key))
            {
                if (!std::binary_search(values.begin(), values.end(), new_value)) {
                    children[mid].second->insert(new_key, new_value);
                }
                return;
            }
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
        values.push_back(new_value);
    }

    void finalize_offset()
    {
        for (auto child : children)
             child.second->finalize_offset();

        if (inserted_children.size() > 0) {
            PatriciaKey<KeyType> current_key = inserted_children[0].first;
            PatriciaNode<KeyType, ValueType> *current_node = new PatriciaNode<KeyType, ValueType>(values, inserted_children[0].second);
            for (unsigned i = 1; i < inserted_children.size(); i++) {
                if (inserted_children[i].first == current_key)
                    current_node->add(inserted_children[i].second);
                else {
                    children.push_back(Child(current_key, current_node));
                    current_key = inserted_children[i].first;
                    current_node = new PatriciaNode<KeyType, ValueType>(values, inserted_children[i].second);
                }
            }
            children.push_back(Child(current_key, current_node));
            inserted_children.clear();
            std::sort(children.begin(), children.end());
        }
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
    std::vector<Child> children;
    std::vector<ValueType> values;

    // This is to optimize insertion of new nodes
    //std::set<ValueType> creation_set;
    std::vector<InsertedChild> inserted_children;
};

template <typename KeyType, typename ValueType>
class Patricia {
public:

    typedef PatriciaElem<KeyType, ValueType> Elem;
    typedef PatriciaKey<KeyType> Key;

    Patricia(std::vector<Elem>& elems)
    {
        uint8_t current_offset;

        std::sort(elems.begin(), elems.end());
        elems.erase( std::unique( elems.begin(), elems.end() ), elems.end() );

        current_offset = elems[0].offset;

        for (Elem &el : elems) {
            if (el.offset < current_offset) {
                root.finalize_offset();
                current_offset = el.offset;
            }
            Key key(el.key, el.offset);
            root.insert(key, el.value);
        }

        root.finalize_offset();
    }

    std::vector<ValueType> *
    lookup (KeyType key)
    {
        return root.lookup(key);
    }

private:
    PatriciaNode<KeyType, ValueType> root;

};
