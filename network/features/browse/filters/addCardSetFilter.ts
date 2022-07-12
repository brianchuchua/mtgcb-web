import { CardSet } from '../../../../features/browse/browseSlice';

interface AddCardSetFilterFunction {
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  (cardSets: CardSet[], where: { AND: any[] }): any;
}

const addCardSetFilter: AddCardSetFilterFunction = (cardSets, where) => {
  if (cardSets?.length) {
    const setIdConditions = { setId: { AND: [], OR: [] } };
    for (const cardSetSelection of cardSets) {
      if (cardSetSelection.exclude) {
        setIdConditions.setId.AND.push({ id_not: cardSetSelection.value });
      } else {
        setIdConditions.setId.OR.push({ id: cardSetSelection.value });
      }
    }
    if (setIdConditions.setId.OR.length === 0) {
      delete setIdConditions.setId.OR;
    }
    where.AND.push(setIdConditions);
  }
};

export default addCardSetFilter;
