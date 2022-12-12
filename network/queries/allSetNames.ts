const allSetNames = `query allSets($orderBy: [SetOrderByInput!] = [{releasedAt: desc}]) {
  sets(orderBy: $orderBy) {
    id
    name
  }
}
`;

export default allSetNames;
