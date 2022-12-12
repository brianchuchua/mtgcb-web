const allCardsMeta = `query allCardsCount($where: CardWhereInput = {}) {
  count:cardsCount(where: $where)
}
`;

export default allCardsMeta;
