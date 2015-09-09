#include <algorithm>
#include <cstdint>
#include <vector>
#include <iostream>
#include <set>

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

    bool operator!=(const PatriciaKey<KeyType>& key) const {
        return value_ != key.value_ || offset_ != key.offset_;
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

    bool applies_to(const PatriciaKey<KeyType>& patkey) const {
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
    typedef std::pair<PatriciaKey<KeyType>, PatriciaNode> Child;

public:
    PatriciaNode() : finalized_children(0) {}

    PatriciaNode(std::vector<ValueType>& parent_values) : values(parent_values), finalized_children(0) {}

    void
    insert(PatriciaKey<KeyType>& new_key, ValueType new_value)
    {
        int min = 0;
        int max = finalized_children - 1;

        if (std::binary_search(values.begin(), values.end(), new_value))
            return;

        while (max >= min)
        {
            int mid = min + ((max - min) / 2);
            PatriciaKey<KeyType> current_key = children[mid].first;
            if (current_key.applies_to(new_key))
            {
                children[mid].second.insert(new_key, new_value);
                return;
            }
            else if(new_key.value() < current_key.value())
                max = mid - 1;
            else
                min = mid + 1;
        }

        if (finalized_children == children.size() || children.back().first != new_key) {
            children.emplace_back(new_key, PatriciaNode(values));
        }

        children.back().second.add(new_value);
    }

    void
    add(ValueType new_value)
    {
        values.emplace_back(new_value);
    }

    void finalize()
    {
        std::sort(values.begin(), values.end());
    }

    void finalize_offset()
    {
        for (auto it = children.begin(); it != children.begin() + finalized_children; it++) {
            (*it).second.finalize_offset();
        }

        if (finalized_children < children.size()) {
            for(auto it = children.begin() + finalized_children; it != children.end(); it++) {
                (*it).second.finalize();
            }
            std::sort(children.begin(), children.end());
            finalized_children = children.size();
        }
    }

    std::vector<ValueType>
    lookup(const PatriciaKey<KeyType>& lookup_key)
    {

        int min = 0;
        int max = children.size()-1;

        while (max >= min)
        {
            int mid = min + ((max - min) / 2);
            PatriciaKey<KeyType> current_key = children[mid].first;
            if (current_key.applies_to(lookup_key))
            {
                return children[mid].second.lookup(lookup_key);
            }
            else if(lookup_key.value() < current_key.value())
                max = mid - 1;
            else
                min = mid + 1;
        }

        return values;
    }

    bool operator<(const PatriciaNode& other) const
    {
        return (&values) < (&other.values);
    }

private:
    std::vector<Child> children;
    std::vector<ValueType> values;
    unsigned finalized_children;
};

template <typename KeyType, typename ValueType>
class Patricia {
public:

    typedef PatriciaElem<KeyType, ValueType> Elem;
    typedef PatriciaKey<KeyType> Key;

    Patricia() { }

    Patricia(std::vector<Elem> elems)
    {
        if (elems.size() > 0) {
            std::sort(elems.begin(), elems.end());
            elems.erase( std::unique( elems.begin(), elems.end() ), elems.end() );

            uint8_t current_offset = elems[0].offset;

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
    }

    std::vector<ValueType>
    lookup (KeyType key, uint8_t offset)
    {
        PatriciaKey<KeyType> lookup_key(key, offset);
        return root.lookup(lookup_key);
    }

private:
    PatriciaNode<KeyType, ValueType> root;

};
