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
          { oracleTypeLine_not_contains_i: `${cardTypeSelection.value} ` },
          { oracleTypeLine_not_ends_with_i: `${cardTypeSelection.value}` },
        ],
      });
    } else {
      where.AND.push({
        OR: [{ oracleTypeLine_contains_i: `${cardTypeSelection.value} ` }, { oracleTypeLine_ends_with_i: `${cardTypeSelection.value}` }],
      });
    }
  }
};

export default addCardTypeFilter;
