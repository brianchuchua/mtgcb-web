const allCards = `query allCards($take: Int = 50, $skip: Int = 0, $orderBy: [CardOrderByInput!] = [{name: asc},{releasedAt: asc}], $where: CardWhereInput = {}) {
  cards(take: $take, skip: $skip, orderBy: $orderBy, where: $where) {
    id
    name
    set:setId {
      id
      name
      slug
    }
    rarity
    manaCost
    convertedManaCost
    oracleTypeLine
    collectorNumber
    mtgcbCollectorNumberNumeric
    tcgplayerId
    priceId {
      low
      average
      high
      market
      foil
    }
  }
}
`;

export default allCards;
