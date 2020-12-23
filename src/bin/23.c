#include <stdlib.h>
#include <stdio.h>
#include <time.h>

#define START 4
#define INPUT {4,6,3,5,2,8,1,7,9}

typedef unsigned long u64;

typedef struct Node {
  u64 val;
  struct Node *right;
} Node;

Node* build_list(int size) {
  u64 input[] = INPUT;
  Node *list = calloc(sizeof(Node), size);

  Node *prev = list + START;
  prev->val = START;

  for (int i = 1; i < size; ++i) {
    u64 val = i < 9 ? input[i] : i + 1;
    Node *n = list + val;
    n->val = val;
    prev->right = n;
    prev = n;
  }
  prev->right = list + START;

  return list;
}

Node* simulate(int size, int rounds) {
  Node *list = build_list(size);
  Node *curr = list + START;
  for (int i = 0; i < rounds; curr = curr->right, ++i) {
    Node *a = curr->right;
    Node *b = curr->right->right;
    Node *c = curr->right->right->right;

    int t = curr->val;
    do {
      if (--t == 0) t = size;
    } while (t == a->val || t == b->val || t == c->val);
    Node *dest = list + t;

    curr->right = c->right;
    c->right = dest->right;
    dest->right = a;
  }
  return list;
}

u64 part_one(void) {
  Node *list = simulate(9, 100);
  Node *n = (list + 1)->right;
  u64 ans = 0;
  while (n->val != 1) {
    ans *= 10;
    ans += n->val;
    n = n->right;
  };
  return ans;
}

u64 part_two(void) {
  Node *list = simulate(1000000, 10000000);
  Node *one = list + 1;
  return one->right->val * one->right->right->val;
}

int main() {
  printf("Part one: %ld\n", part_one());
  printf("Part two: %ld\n", part_two());
  printf("Time: %ldms\n", clock() / (CLOCKS_PER_SEC / 1000));
}
