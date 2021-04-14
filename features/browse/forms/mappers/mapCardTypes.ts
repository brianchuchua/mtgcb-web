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

interface MappedCardTypes {
  categoryLabel: string;
  cardType: string;
  exclude: boolean;
}

const mapCardTypes: MapCardTypesFunction = (cardTypesFromApi) => {
  let allCardTypesMapped = [];

  allCardTypesMapped = allCardTypesMapped.concat(
    cardTypesFromApi.cardTypes.map((cardType) => ({ categoryLabel: 'Card Types', cardType, exclude: false })),
    cardTypesFromApi.superTypes.map((cardType) => ({ categoryLabel: 'Supertypes', cardType, exclude: false })),
    cardTypesFromApi.artifactTypes.map((cardType) => ({ categoryLabel: 'Artifact Types', cardType, exclude: false })),
    cardTypesFromApi.enchantmentTypes.map((cardType) => ({ categoryLabel: 'Enchantment Types', cardType, exclude: false })),
    cardTypesFromApi.landTypes.map((cardType) => ({ categoryLabel: 'Land Types', cardType, exclude: false })),
    cardTypesFromApi.spellTypes.map((cardType) => ({ categoryLabel: 'Spell Types', cardType, exclude: false })),
    cardTypesFromApi.creatureTypes.map((cardType) => ({ categoryLabel: 'Creature Types', cardType, exclude: false })),
    cardTypesFromApi.planarTypes.map((cardType) => ({ categoryLabel: 'Planar Types', cardType, exclude: false })),
    cardTypesFromApi.planeswalkerTypes.map((cardType) => ({ categoryLabel: 'Planeswalker Types', cardType, exclude: false }))
  );

  return allCardTypesMapped;
};

export default mapCardTypes;
