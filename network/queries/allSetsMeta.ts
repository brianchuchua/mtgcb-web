const allSetsMeta = `query _allSetsMeta($name: String = "", $sortBy: [SortSetsBy!] = [releasedAt_DESC], $where: SetWhereInput = {}) {
  _allSetsMeta(search: $name, sortBy: $sortBy, where: $where) {
    count
  }
}
`;

export default allSetsMeta;
