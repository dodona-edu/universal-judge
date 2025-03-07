class Counter {
private:
    int counter;

public:
    // Constructor
    Counter() : counter(0) {}

    // Method to add to the counter
    Counter& add() {
        counter++;
        return *this;
    }

    // Method to get the current counter value
    int get() const {
        return counter;
    }
};
