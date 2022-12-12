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
      if (searchAttribute === 'cardsAll' || searchAttribute === 'cardsNormal' || searchAttribute === 'cardsFoil') {
        // eslint-disable-next-line no-continue
        continue;
      }
      if (value !== '') {
        if (comparator === 'eq') {
          let equalitySearchAttribute = searchAttribute as string;
          if (valueIsNotNumeric(value)) {
            equalitySearchAttribute = searchAttribute.replace('Numeric', '');
          }
          cardStatSearchConditions.AND.push({
            [equalitySearchAttribute]: { equals: value },
          });
        } else {
          cardStatSearchConditions.AND.push({
            [searchAttribute]: { [comparator]: value },
          });
        }
      }
    }
    if (cardStatSearchConditions.AND.length > 0) {
      where.AND.push(cardStatSearchConditions);
    }
  }
};

const valueIsNotNumeric = (value) => {
  const possiblyNumericValue = Number(value);
  return Number.isNaN(possiblyNumericValue);
};

export default addCardStatFilter;
