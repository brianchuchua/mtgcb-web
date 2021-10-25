const allSetNames = `query allSets($sortBy: [SortSetsBy!] = releasedAt_DESC) {
  allSets(sortBy: $sortBy) {
    id
    name
  }
}
`;

export default allSetNames;
