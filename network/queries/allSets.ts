const allSets = `query allSets($first: Int = 50, $skip: Int = 0, $sortBy: [SortSetsBy!] = releasedAt_DESC) {
  allSets(first: $first, skip: $skip, sortBy: $sortBy) {
    id
    name
    code
    category
    setType
    cardCount
    releasedAt
  }
}
`;

export default allSets;
