const allCards = `query allCards($first: Int = 50, $skip: Int = 0, $sortBy: [SortCardsBy!] = name_ASC, $name: String = "") {
  allCards(first: $first, skip: $skip, sortBy: $sortBy, search: $name) {
    id
    name
    set:setId {
      name
    }
  }
}
`;

export default allCards;
