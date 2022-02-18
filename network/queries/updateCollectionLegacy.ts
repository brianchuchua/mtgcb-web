const updateCollectionLegacy = `mutation updateCollectionLegacy($cardId: Int!, $mode: String!, $quantityRegular: Int, $quantityFoil: Int){
  updateCollectionLegacy(cardId: $cardId, mode: $mode, quantityRegular: $quantityRegular, , quantityFoil: $quantityFoil){
  	cardId
    quantityRegular
    quantityFoil
  }
}
`;

export default updateCollectionLegacy;
