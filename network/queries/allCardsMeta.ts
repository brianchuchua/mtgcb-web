const allCardsMeta = `query _allCardsMeta($name: String = "", $where: CardWhereInput = {}, $sortBy: [SortCardsBy!] = [name_ASC, releasedAt_ASC], $distinct: [String] = "") {
  _allCardsMeta(search: $name, where: $where, sortBy: $sortBy, distinct: $distinct) {
    count
  }
}
`;

export default allCardsMeta;
