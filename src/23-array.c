#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

unsigned long CUPS = 1000000;
unsigned long MOVES = 10000000;

// Precondition: 1 <= cup <= CUPS
unsigned long prevCup(unsigned long cup) {
    if (cup == 1) {
        return CUPS;
    } else {
        return cup - 1;
    }
}

unsigned long play(unsigned long* cups, unsigned long cup) {
    unsigned long first = cups[cup];
    unsigned long second = cups[first];
    unsigned long third = cups[second];
    unsigned long fourth = cups[third];
    unsigned long dest = prevCup(cup);
    while (dest == first || dest == second || dest == third) {
        dest = prevCup(dest);
    }
    cups[cup] = fourth;
    cups[third] = cups[dest];
    cups[dest] = first;
    return fourth;
}

int main() {
    int input[9] = {3, 6, 2, 9, 8, 1, 7, 5, 4};
    int arr[9] = {7, 9, 6, 10, 4, 2, 5, 1, 8};
    unsigned long* cups = malloc((CUPS + 1) * sizeof(*cups));
    unsigned long i;
    unsigned long cup = 3;
    cups[0] = 0;
    for (i = 1; i <= 9; i++) {
        cups[i] = arr[i - 1];
    }
    for (i = 10; i <= CUPS; i++) {
        cups[i] = i + 1;
    }
    for (i = 0; i < MOVES; i++) {
        cup = play(cups, cup);
    }
    unsigned long first = cups[1];
    unsigned long second = cups[first];
    printf("Product of two cups after cup 1 is %lu * %lu = %lu\n", first, second, first * second);
}
