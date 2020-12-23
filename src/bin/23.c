#include <stdlib.h>
#include <stdio.h>
#include <time.h>

#define START 4

typedef struct Node {
  int val;
  struct Node *next;
} Node;

Node* build_list(int size) {
  int input[] = {4,6,3,5,2,8,1,7,9};
  Node *list = malloc(sizeof(Node) * size);

  Node *prev = list + START;
  prev->val = START;

  for (int i = 1; i < size; ++i) {
    int val = i < 9 ? input[i] : i + 1;
    Node *n = list + val;
    n->val = val;
    prev->next = n;
    prev = n;
  }
  prev->next = list + START;

  return list;
}

Node* simulate(int size, int rounds) {
  Node *list = build_list(size);
  Node *curr = list + START;
  for (int i = 0; i < rounds; curr = curr->next, ++i) {
    Node *a = curr->next;
    Node *b = curr->next->next;
    Node *c = curr->next->next->next;

    int t = (curr->val == 1 ? size : curr->val - 1);
    while (t == a->val || t == b->val || t == c->val)
      t = (t == 1 ? size : t - 1);
    Node *dest = list + t;

    curr->next = c->next;
    c->next = dest->next;
    dest->next = a;
  }
  return list + 1;
}

int part_one(void) {
  Node *one = simulate(9, 100);
  int ans = 0;
  for (Node *n = one->next; n->val != 1; n = n->next)
    ans = ans * 10 + n->val;
  return ans;
}

long part_two(void) {
  Node *one = simulate(1000000, 10000000);
  return one->next->val * one->next->next->val;
}

int main() {
  printf("Part one: %d\n", part_one());
  printf("Part two: %ld\n", part_two());
  printf("Time: %ldms\n", clock() / (CLOCKS_PER_SEC / 1000));
}
