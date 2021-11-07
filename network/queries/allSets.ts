const allSets = `query allSets($first: Int = 50, $skip: Int = 0, $name: String = "", $sortBy: [SortSetsBy!] = releasedAt_DESC, $where: SetWhereInput = {}) {
  allSets(first: $first, skip: $skip, search: $name, sortBy: $sortBy, where: $where) {
    id
    name
    code
    category
    setType
    cardCount
    releasedAt
    sealedProductUrl
    isDraftable
  }
}
`;

export default allSets;
