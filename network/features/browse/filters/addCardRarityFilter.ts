import { CardRarity } from '../../../../features/browse/browseSlice';

interface AddCardSetFilterFunction {
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  (cardRarities: CardRarity[], where: { AND: any[] }): any;
}

const addCardRarityFilter: AddCardSetFilterFunction = (cardRarities, where) => {
  const cardRarityConditions = { AND: [], OR: [] };

  if (cardRarities.length) {
    for (const cardRarity of cardRarities) {
      const rarity = cardRarity.value === 'none' ? null : cardRarity.value;
      if (cardRarity.exclude) {
        cardRarityConditions.AND.push({ rarity: { not: rarity, mode: 'insensitive' } });
      } else {
        cardRarityConditions.OR.push({ rarity: { equals: rarity, mode: 'insensitive' } });
      }
    }

    if (cardRarityConditions.OR.length === 0) {
      delete cardRarityConditions.OR;
    }
    where.AND.push(cardRarityConditions);
  }
};

export default addCardRarityFilter;
