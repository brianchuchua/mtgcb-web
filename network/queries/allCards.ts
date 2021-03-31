const allCards = `query allCards($first: Int = 50, $skip: Int = 0, $sortBy: [SortCardsBy!] = name_ASC, $name: String = "", $where: CardWhereInput = {}) {
  allCards(first: $first, skip: $skip, sortBy: $sortBy, search: $name, where: $where) {
    id
    name
    set:setId {
      name
    }
  }
}
`;

export default allCards;
