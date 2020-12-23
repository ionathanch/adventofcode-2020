#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

int CUPS = 1000000;
int MOVES = 10000000;

struct link {
    int cup;
    struct link* next;
};

// Precondition: head is not NULL
void freeLoop(struct link* head) {
    struct link* curr = head;
    do {
        struct link* next = curr->next;
        free(curr);
        curr = next;
    } while (curr && curr != head);
}

// Precondition: head is not NULL
void printLoop(struct link* head) {
    struct link* curr = head;
    do {
        printf("%d", curr->cup);
        curr = curr->next;
    } while (curr && curr != head);
    printf("\n");
}

// Builds a chain starting from the given link,
// with each subsequent link containing ints start to end
// Returns the last link in the chain
struct link* buildChain(struct link* curr, int start, int end) {
    for (int i = start; i <= end; i++) {
        struct link* next = malloc(sizeof(struct link));
        next->cup = i;
        next->next = NULL;
        curr->next = next;
        curr = next;
    }
    return curr;
}

// Builds a chain from the given ints in the array,
// setting the given link address to point to the first link
// Returns the last link in the chain
// Precondition: arr has at least one int
struct link* buildArrayChain(struct link** head, int* arr, int length) {
    *head = malloc(sizeof(struct link));
    struct link* curr = *head;
    curr->cup = arr[0];
    for (int i = 1; i < length; i++) {
        struct link* next = malloc(sizeof(struct link));
        next->cup = arr[i];
        next->next = NULL;
        curr->next = next;
        curr = next;
    }
    return curr;
}

// Returns the link in the chain with the given cup
// Precondition: The cup must exist in the chain
struct link* findLink(struct link* curr, int cup) {
    while (curr->cup != cup) {
        curr = curr->next;
    }
    return curr;
}

// Precondition: 1 <= cup <= CUPS
int prevCup(int cup) {
    if (cup == 1) {
        return CUPS;
    } else {
        return cup - 1;
    }
}

struct link* move(struct link* head) {
    struct link* firstLink = head->next;
    struct link* secondLink = head->next->next;
    struct link* thirdLink = head->next->next->next;
    int first = firstLink->cup;
    int second = secondLink->cup;
    int third = thirdLink->cup;
    head->next = thirdLink->next;

    int destCup = prevCup(head->cup);
    while (destCup == first || destCup == second || destCup == third) {
        destCup = prevCup(destCup);
    }

    struct link* dest = findLink(head, destCup);
    thirdLink->next = dest->next;
    dest->next = firstLink;
    return head->next;
}

int main() {
    int arr[9] = {3, 6, 2, 9, 8, 1, 7, 5, 4};
    int length = 9;
    struct link* head;
    struct link* tail = buildArrayChain(&head, arr, length);
    tail = buildChain(tail, 10, 1000000);
    tail->next = head;
    printf("Loop has been built.\n");

    for (int i = 0; i < MOVES; i++) {
        head = move(head);
    }
    head = findLink(head, 1);

    int first = head->next->cup;
    int second = head->next->next->cup;
    printf("Product of two cups after cup 1 is %d * %d = %d\n", first, second, first * second);
    freeLoop(head);
}
