const cardAutocomplete = `query cardAutocomplete($take: Int = 500, $orderBy: [CardOrderByInput!] = [{name: asc},{releasedAt: desc}], $where: CardWhereInput ={}) {
  cards(take: $take, orderBy: $orderBy, where: $where) {
    id
    name
    priceId {
      low
      average
      high
      market
      foil
    }
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
