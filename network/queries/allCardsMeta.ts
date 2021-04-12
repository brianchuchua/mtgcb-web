const allCardsMeta = `query _allCardsMeta($name: String = "", $where: CardWhereInput = {}, $distinct: String = "") {
  _allCardsMeta(search: $name, where: $where, distinct: $distinct) {
    count
  }
}
`;

export default allCardsMeta;
