const collectionByCardIdLegacy = `query collectionByCardIdLegacy($userId: Int!, $cardIds: [Int]!){
  collectionByCardIdLegacy(userId: $userId, cardIds: $cardIds) {
      userId
      collection {
          cardID
          quantityReg
          quantityFoil
      }
  }
}`;

export default collectionByCardIdLegacy;
