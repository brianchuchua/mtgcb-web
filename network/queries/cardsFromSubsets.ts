const cardsFromSubsets = `query cardsFromSubsets($where: CardWhereInput = {}, $orderBy: [CardOrderByInput!] = [{releasedAt: asc}]) {
  cards(where: $where, orderBy: $orderBy) {
    id
    name
    set:setId {
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

export default cardsFromSubsets;
