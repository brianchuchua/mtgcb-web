interface MapCardTypesFunction {
  (cardTypesFromApi: CardTypes): MappedCardTypes[];
}

interface CardTypes {
  cardTypes: string[];
  superTypes: string[];
  artifactTypes: string[];
  enchantmentTypes: string[];
  landTypes: string[];
  spellTypes: string[];
  creatureTypes: string[];
  planarTypes: string[];
  planeswalkerTypes: string[];
}

export interface MappedCardTypes {
  category: string;
  label: string;
  value: string;
  exclude: boolean;
}

const mapCardTypes: MapCardTypesFunction = (cardTypesFromApi) => {
  let allCardTypesMapped = [];

  allCardTypesMapped = allCardTypesMapped.concat(
    cardTypesFromApi.cardTypes.map((type) => ({ category: 'Card Types', label: type, value: type, exclude: false })),
    cardTypesFromApi.superTypes.map((type) => ({ category: 'Supertypes', label: type, value: type, exclude: false })),
    cardTypesFromApi.artifactTypes.map((type) => ({ category: 'Artifact Types', label: type, value: type, exclude: false })),
    cardTypesFromApi.enchantmentTypes.map((type) => ({ category: 'Enchantment Types', label: type, value: type, exclude: false })),
    cardTypesFromApi.landTypes.map((type) => ({ category: 'Land Types', label: type, value: type, exclude: false })),
    cardTypesFromApi.spellTypes.map((type) => ({ category: 'Spell Types', label: type, value: type, exclude: false })),
    cardTypesFromApi.creatureTypes.map((type) => ({ category: 'Creature Types', label: type, value: type, exclude: false })),
    cardTypesFromApi.planarTypes.map((type) => ({ category: 'Planar Types', label: type, value: type, exclude: false })),
    cardTypesFromApi.planeswalkerTypes.map((type) => ({ category: 'Planeswalker Types', label: type, value: type, exclude: false }))
  );

  return allCardTypesMapped;
};

export default mapCardTypes;
