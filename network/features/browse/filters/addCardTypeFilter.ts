import { CardType } from '../../../../features/browse/browseSlice';

const addCardTypeFilter = (cardTypes: CardType[], where: { AND: any[] }) => {
  // The _contains with a trailing space and _not_ends_with pattern forces Keystone to find exact matches instead of partial.
  // (The string being searched either has a space after it or is the last string in the type line.)
  for (const cardTypeSelection of cardTypes) {
    if (cardTypeSelection.exclude) {
      where.AND.push({
        AND: [
          { oracleTypeLine_not_contains_i: `${cardTypeSelection.cardType} ` },
          { oracleTypeLine_not_ends_with_i: `${cardTypeSelection.cardType}` },
        ],
      });
    } else {
      where.AND.push({
        OR: [
          { oracleTypeLine_contains_i: `${cardTypeSelection.cardType} ` },
          { oracleTypeLine_ends_with_i: `${cardTypeSelection.cardType}` },
        ],
      });
    }
  }
};

export default addCardTypeFilter;
