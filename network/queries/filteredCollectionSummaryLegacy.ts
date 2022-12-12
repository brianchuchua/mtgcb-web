const filteredCollectionSummaryLegacy = `query filteredCollectionSummaryLegacy(
  $userId: Int!,
  $priceType: PriceType = market, 
  $take: Int = 50, 
  $skip: Int = 0, 
  $search: String = "", 
  $orderBy: [SetOrderByInput!] = [{releasedAt: desc}], 
  $additionalSortBy: AdditionalSortSetsBy, 
  $whereSetCompletionStatus: [SetCompletionWhereInput],
  $where: SetWhereInput = {}) {
  filteredCollectionSummaryLegacy(userId: $userId, priceType: $priceType, take: $take, skip: $skip, 
    search: $search, orderBy: $orderBy, additionalSortBy: $additionalSortBy, whereSetCompletionStatus: $whereSetCompletionStatus, where: $where) {
      userId
      username
      totalCardsCollected
      uniquePrintingsCollected
      numberOfCardsInMagic
      percentageCollected
      count
      totalValue {
        market
        low
        average
        high
      }
      collectionSummary { 
        id
        setId
        name
        slug
        code
        category
        setType
        cardCount
        releasedAt
        sealedProductUrl
        isDraftable
        cardsInSet
        totalCardsCollectedInSet
        uniquePrintingsCollectedInSet
        percentageCollected
        market {
          oneOfEachCard
          oneOfEachMythic
          oneOfEachRare
          oneOfEachUncommon
          oneOfEachCommon
          fourOfEachCard
          fourOfEachMythic
          fourOfEachRare
          fourOfEachUncommon
          fourOfEachCommon
          draftCube
          totalValue
        }
        low {
          oneOfEachCard
          oneOfEachMythic
          oneOfEachRare
          oneOfEachUncommon
          oneOfEachCommon
          fourOfEachCard
          fourOfEachMythic
          fourOfEachRare
          fourOfEachUncommon
          fourOfEachCommon
          draftCube
          totalValue
        }
        average {
          oneOfEachCard
          oneOfEachMythic
          oneOfEachRare
          oneOfEachUncommon
          oneOfEachCommon
          fourOfEachCard
          fourOfEachMythic
          fourOfEachRare
          fourOfEachUncommon
          fourOfEachCommon
          draftCube
          totalValue
        }
        high {
          oneOfEachCard
          oneOfEachMythic
          oneOfEachRare
          oneOfEachUncommon
          oneOfEachCommon
          fourOfEachCard
          fourOfEachMythic
          fourOfEachRare
          fourOfEachUncommon
          fourOfEachCommon
          draftCube
          totalValue
        }
      }
    }
  }`;

export default filteredCollectionSummaryLegacy;
