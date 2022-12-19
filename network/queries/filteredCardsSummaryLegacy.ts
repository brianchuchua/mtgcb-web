const filteredCardsSummaryLegacy = `query filteredCardsSummaryLegacy(
  $userId: Int!,
  $setId: Int,
  $where: CardWhereInput = {},
  $search: String = "",
  $take: Int = 50,
  $skip: Int = 0,
  $orderBy: [CardOrderByInput!] = [{releasedAt: asc}, {name: asc}],
  $additionalSortBy: AdditionalSortCardsBy, 
  $additionalWhere: AdditionalWhereInput) {
    filteredCardsSummaryLegacy(userId: $userId, setId: $setId, where: $where, search: $search, take: $take, skip: $skip, orderBy: $orderBy, additionalSortBy: $additionalSortBy, additionalWhere: $additionalWhere) {
      userId
      cards {
        id
        name
        set {
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
        quantityReg
        quantityFoil
      }
      count 
    }
  }`;

export default filteredCardsSummaryLegacy;
