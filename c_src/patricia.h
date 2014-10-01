#include <cstdint>
#include <vector>
#include <iostream>
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
    std::vector<PatriciaKey<KeyType>> keys_;
    ValueType value_;

};

template <typename KeyType, typename ValueType>
class PatriciaNode
{
public:
    PatriciaNode() {}

    PatriciaNode(ValueType value)
    {
        values.push_back(value);
    }

    PatriciaNode(ValueType value, PatriciaKey<KeyType> child_key, PatriciaNode *child)
    {
        values.push_back(value);
        keys.push_back(child_key);
        children.push_vack(child);
    }

    ~PatriciaNode()
    {
        unsigned size = children.size();
        for (unsigned i = 0; i < size; i++)
        {
            delete children[i];
        }
    }

    void
    insert(PatriciaKey<KeyType>& new_key, ValueType& new_value)
    {
        int min = 0;
        int max = keys.size()-1;

        /* cout << "IP : " << new_key.value() << endl; */
        /* cout << "Mask : " << new_key.offset() << endl; */
        /* cout << "Max : " << max << endl; */

        while (max >= min)
        {
            int mid = min + ((max - min) / 2);
            PatriciaKey<KeyType> current_key = keys[mid];
            if (current_key == new_key)
            {
                children[mid]->add(new_value);
                return;
            }
            else if (current_key.applies_to(new_key))
            {
                children[mid]->insert(new_key, new_value);
                return;
            }
            // new_key should never apply to current_key if insertion order is respected
            else if(new_key.value() < current_key.value())
                max = mid - 1;
            else
                min = mid + 1;
        }

        PatriciaNode<KeyType,ValueType> *new_node = new PatriciaNode<KeyType,ValueType>(new_value);
        keys.insert(keys.begin()+min, new_key);
        children.insert(children.begin()+min, new_node);
    }

    void
    add(ValueType new_value)
    {
        values.push_back(new_value);
    }

    void
    lookup(KeyType key, std::vector<ValueType>& results)
    {

        results.insert(results.end(), values.begin(), values.end());

        int min = 0;
        int max = keys.size()-1;

        while (max >= min)
        {
            int mid = min + ((max - min) / 2);
            PatriciaKey<KeyType> current_key = keys[mid];
            if (current_key.applies_to(key))
            {
                children[mid]->lookup(key, results);
                return;
            }
            else if(key < current_key.value())
                max = mid - 1;
            else
                min = mid + 1;
        }
    }


private:
    std::vector<ValueType> values;
    std::vector<PatriciaKey<KeyType>> keys;
    std::vector<PatriciaNode *> children;
};

template <typename KeyType, typename ValueType>
class Patricia {
public:

    Patricia(std::vector<PatriciaPair<KeyType, ValueType>>& pairs)
    {
        unsigned length = pairs.size();
        n_values = length;
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
                        PatriciaKey<KeyType> current_key = pairs[i].get_key(j);
                        root.insert(current_key, value);
                    }
                }
            }
        }
    }

    void
    lookup (KeyType key, std::vector<ValueType>& results)
    {
        results.reserve(n_values);
        root.lookup(key, results);
    }

private:
    unsigned n_values;
    PatriciaNode<KeyType, ValueType> root;

};
