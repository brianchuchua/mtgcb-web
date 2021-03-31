const allCardsMeta = `query _allCardsMeta($name: String = "", $where: CardWhereInput = {}) {
  _allCardsMeta(search: $name, where: $where) {
    count
  }
}
`;

export default allCardsMeta;
