#include <stdlib.h>
#include <stdio.h>
#include <time.h>

#define VAL(n) (n - list)
int INPUT[] = {4,6,3,5,2,8,1,7,9};

typedef struct Node { struct Node *next; } Node;

Node* build_list(int size) {
  Node *list = malloc(sizeof(Node) * size);
  Node *curr = list + INPUT[0];
  for (int i = 1; i < size; curr = curr->next, ++i) {
    int val = i < 9 ? INPUT[i] : i + 1;
    curr->next = list + val;
  }
  curr->next = list + INPUT[0];
  return list;
}

Node* simulate(int size, int rounds) {
  Node *list = build_list(size);
  Node *curr = list + INPUT[0];
  for (int i = 0; i < rounds; curr = curr->next, ++i) {
    Node *a = curr->next, *b = a->next, *c = b->next;
    int t = (VAL(curr) == 1 ? size : VAL(curr) - 1);
    while (t == VAL(a) || t == VAL(b) || t == VAL(c))
      t = (t == 1 ? size : t - 1);
    Node *dest = list + t;

    curr->next = c->next;
    c->next = dest->next;
    dest->next = a;
  }
  return list;
}

int part_one(void) {
  Node *list = simulate(9, 100);
  int ans = 0;
  for (Node *n = list[1].next; VAL(n) != 1; n = n->next)
    ans = ans * 10 + VAL(n);
  return ans;
}

long part_two(void) {
  Node *list = simulate(1000000, 10000000);
  return VAL(list[1].next) * VAL(list[1].next->next);
}

int main() {
  printf("Part one: %d\n", part_one());
  printf("Part two: %ld\n", part_two());
  printf("Time: %ldms\n", clock() / (CLOCKS_PER_SEC / 1000));
}
