const allSetsMeta = `query allSetsCount($where: SetWhereInput = {}) {
  count:setsCount(where: $where)
}
`;

export default allSetsMeta;
