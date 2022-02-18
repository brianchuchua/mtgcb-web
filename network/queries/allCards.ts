const allCards = `query allCards($first: Int = 50, $skip: Int = 0, $sortBy: [SortCardsBy!] = [name_ASC, releasedAt_ASC], $name: String = "", $where: CardWhereInput = {}, $distinct: [String] = "") {
  allCards(first: $first, skip: $skip, sortBy: $sortBy, search: $name, where: $where, distinct: $distinct) {
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
    tcgplayerId
    low
    average
    high
    market
    foil
  }
}
`;

export default allCards;
