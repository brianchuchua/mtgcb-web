const cardAutocomplete = `query cardAutocomplete($first: Int = 50, $sortBy: [SortCardsBy!] = [name_ASC, releasedAt_ASC], $name: String = "") {
  allCards(first: $first, sortBy: $sortBy, search: $name) {
    id
    name
    low
    average
    high
    market
    foil
    tcgplayerId
    set:setId {
      id
      name
      slug
    }
  }
}
`;

export default cardAutocomplete;
