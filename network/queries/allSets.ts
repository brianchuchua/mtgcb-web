const allSets = `query allSets($take: Int = 50, $skip: Int = 0, $orderBy: [SetOrderByInput!] = [{releasedAt: desc}], $where: SetWhereInput = {}) {
  sets(take: $take, skip: $skip, orderBy: $orderBy, where: $where) {
    id
    name
    slug
    code
    category
    setType
    cardCount
    releasedAt
    sealedProductUrl
    isDraftable
    subsetGroupId {
      id
      name
    }
    parentSetId {
      id
      name
    }
    isSubsetGroup
  }
}
`;

export default allSets;
