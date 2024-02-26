import { CardType } from '../../../../features/browse/browseSlice';

interface AddCardTypeFilterFunction {
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  (cardTypes: CardType[], where: { AND: any[] }): any;
}

const addCardTypeFilter: AddCardTypeFilterFunction = (cardTypes, where) => {
  // The _contains with a trailing space and _not_ends_with pattern forces Keystone to find exact matches instead of partial.
  // (The string being searched either has a space after it or is the last string in the type line.)
  for (const cardTypeSelection of cardTypes) {
    if (cardTypeSelection.exclude) {
      where.AND.push({
        AND: [
          { oracleTypeLine: { not: { startsWith: `${cardTypeSelection.value} `, mode: 'insensitive' } } },
          { oracleTypeLine: { not: { contains: ` ${cardTypeSelection.value} `, mode: 'insensitive' } } },
          { oracleTypeLine: { not: { endsWith: ` ${cardTypeSelection.value}`, mode: 'insensitive' } } },
          { oracleTypeLine: { not: { equals: cardTypeSelection.value, mode: 'insensitive' } } },
        ],
      });
    } else {
      where.AND.push({
        OR: [
          // This is a way to force an exact match instead of a partial match.
          { oracleTypeLine: { startsWith: `${cardTypeSelection.value} `, mode: 'insensitive' } },
          { oracleTypeLine: { contains: ` ${cardTypeSelection.value} `, mode: 'insensitive' } },
          { oracleTypeLine: { endsWith: ` ${cardTypeSelection.value}`, mode: 'insensitive' } },
          { oracleTypeLine: { equals: cardTypeSelection.value, mode: 'insensitive' } },
        ],
      });
    }
  }
};

export default addCardTypeFilter;
