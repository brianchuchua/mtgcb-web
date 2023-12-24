const allSubsetsByGroupId = `query allSubsetsByGroupId($where: SetWhereInput = {}, $orderBy: [SetOrderByInput!] = [{releasedAt: asc}]) {
  sets(where: $where, orderBy: $orderBy) {
    id
    name
    releasedAt
    slug
    isSubsetGroup
  }
}
`;

export default allSubsetsByGroupId;
