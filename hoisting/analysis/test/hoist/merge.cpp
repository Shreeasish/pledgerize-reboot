#include<iostream>
#include<vector>
#include<algorithm>
#include<iterator>

int main() {
  std::vector<int> expr1 = {1, 2, 3, 4, 9, 10};
  std::vector<int> expr2 = {1, 2, 3, 4, 9, 10};
  std::vector<int> expr3;

  std::set_union(expr1.begin(), expr1.end(),
                 expr2.begin(), expr2.end(),
                 std::back_inserter(expr3));

  std::cout << "\n";
  for (const auto& item : expr3) {
    std::cout << item << " ";
  }
  std::cout << "\n";

  return 0;

}
