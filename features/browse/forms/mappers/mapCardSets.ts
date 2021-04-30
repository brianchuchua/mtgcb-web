interface MapCardSetsFunction {
  (cardSetsFromApi: CardSet[]): MappedCardSets[];
}

interface CardSet {
  id: string;
  name: string;
}

interface MappedCardSets {
  category: string;
  label: string;
  value: string;
  exclude: boolean;
}

const mapCardSets: MapCardSetsFunction = (cardSetsFromApi) => {
  let allCardSetsMapped = [];

  allCardSetsMapped = allCardSetsMapped.concat(
    cardSetsFromApi.map((set: CardSet) => ({ category: 'Sets', label: set.name, value: set.id, exclude: false }))
  );

  return allCardSetsMapped;
};

export default mapCardSets;
