const allSetsMeta = `query _allSetsMeta($sortBy: [SortSetsBy!] = [releasedAt_DESC]) {
  _allSetsMeta(sortBy: $sortBy) {
    count
  }
}
`;

export default allSetsMeta;
