const allSets = `query allSets($sortBy: [SortSetsBy!] = name_ASC) {
  allSets(sortBy: $sortBy) {
    id
    name
  }
}
`;

export default allSets;
