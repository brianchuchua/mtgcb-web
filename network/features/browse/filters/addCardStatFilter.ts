import { CardStatSearch } from '../../../../features/browse/browseSlice';

interface AddCardStatFilterFunction {
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  (cardStatSearches: CardStatSearch[], where: { AND: any[] }): any;
}

const addCardStatFilter: AddCardStatFilterFunction = (cardStatSearches, where) => {
  if (cardStatSearches.length) {
    const cardStatSearchConditions = { AND: [] };
    for (const cardStatSearch of cardStatSearches) {
      const { searchAttribute, comparator, value } = cardStatSearch;
      if (value !== '') {
        if (comparator === 'eq') {
          cardStatSearchConditions.AND.push({
            [searchAttribute]: value,
          });
        } else {
          cardStatSearchConditions.AND.push({
            [`${searchAttribute}_${comparator}`]: value,
          });
        }
      }
    }
    console.log(cardStatSearchConditions);
    if (cardStatSearchConditions.AND.length > 0) {
      where.AND.push(cardStatSearchConditions);
    }
  }
};

export default addCardStatFilter;
