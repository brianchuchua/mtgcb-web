const allSubsets = `query allSubsets($where: SetWhereInput = {}, $orderBy: [SetOrderByInput!] = [{releasedAt: asc}]) {
  sets(where: $where, orderBy: $orderBy) {
    id
    name
    releasedAt
    slug
  }
}
`;

export default allSubsets;
