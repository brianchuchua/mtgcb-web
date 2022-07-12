const filteredCardsSummaryLegacy = `query filteredCardsSummaryLegacy(
  $userId: Int!,
  $setId: Int,
  $where: CardWhereInput = {},
  $search: String = "",
  $first: Int = 50,
  $skip: Int = 0,
  $sortBy: [SortCardsBy!] = [name_ASC, releasedAt_ASC], 
  $additionalSortBy: AdditionalSortCardsBy, 
  $additionalWhere: AdditionalWhereInput) {
    filteredCardsSummaryLegacy(userId: $userId, setId: $setId, where: $where, search: $search, first: $first, skip: $skip, sortBy: $sortBy, additionalSortBy: $additionalSortBy, additionalWhere: $additionalWhere) {
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
