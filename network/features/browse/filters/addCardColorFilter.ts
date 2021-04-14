import { CardColors } from '../../../../features/browse/browseSlice';

interface AddCardColorFilterFunction {
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  (cardTypes: CardColors, where: { AND: any[] }): any;
}

const addCardColorFilter: AddCardColorFilterFunction = (cardColors: CardColors, where) => {
  if (cardColors.type === 'at-least-these-colors') {
    const atLeastTheseColors = { AND: [] };
    if (cardColors.white) {
      atLeastTheseColors.AND.push({ colors_contains: `W` });
    }
    if (cardColors.blue) {
      atLeastTheseColors.AND.push({ colors_contains: `U` });
    }
    if (cardColors.black) {
      atLeastTheseColors.AND.push({ colors_contains: `B` });
    }
    if (cardColors.red) {
      atLeastTheseColors.AND.push({ colors_contains: `R` });
    }
    if (cardColors.green) {
      atLeastTheseColors.AND.push({ colors_contains: `G` });
    }
    where.AND.push(atLeastTheseColors);
  } else if (cardColors.type === 'only-these-colors') {
    const onlyTheseColors = { AND: [] };
    if (cardColors.colorless) {
      onlyTheseColors.AND.push({ colors_not_contains: `W` });
      onlyTheseColors.AND.push({ colors_not_contains: `U` });
      onlyTheseColors.AND.push({ colors_not_contains: `B` });
      onlyTheseColors.AND.push({ colors_not_contains: `R` });
      onlyTheseColors.AND.push({ colors_not_contains: `G` });
    } else {
      if (cardColors.white) {
        onlyTheseColors.AND.push({ colors_contains: `W` });
      } else {
        onlyTheseColors.AND.push({ colors_not_contains: `W` });
      }
      if (cardColors.blue) {
        onlyTheseColors.AND.push({ colors_contains: `U` });
      } else {
        onlyTheseColors.AND.push({ colors_not_contains: `U` });
      }
      if (cardColors.black) {
        onlyTheseColors.AND.push({ colors_contains: `B` });
      } else {
        onlyTheseColors.AND.push({ colors_not_contains: `B` });
      }
      if (cardColors.red) {
        onlyTheseColors.AND.push({ colors_contains: `R` });
      } else {
        onlyTheseColors.AND.push({ colors_not_contains: `R` });
      }
      if (cardColors.green) {
        onlyTheseColors.AND.push({ colors_contains: `G` });
      } else {
        onlyTheseColors.AND.push({ colors_not_contains: `G` });
      }
    }
    where.AND.push(onlyTheseColors);
  } else if (cardColors.type === 'at-most-these-colors') {
    const atMostTheseColors: any = {}; // eslint-disable-line @typescript-eslint/no-explicit-any
    if (cardColors.colorless) {
      atMostTheseColors.AND = [];
      atMostTheseColors.AND.push({ colors_not_contains: `W` });
      atMostTheseColors.AND.push({ colors_not_contains: `U` });
      atMostTheseColors.AND.push({ colors_not_contains: `B` });
      atMostTheseColors.AND.push({ colors_not_contains: `R` });
      atMostTheseColors.AND.push({ colors_not_contains: `G` });
    } else {
      atMostTheseColors.OR = [];
      if (cardColors.white) {
        atMostTheseColors.OR.push({ colors_contains: `W` });
      }
      if (cardColors.blue) {
        atMostTheseColors.OR.push({ colors_contains: `U` });
      }
      if (cardColors.black) {
        atMostTheseColors.OR.push({ colors_contains: `B` });
      }
      if (cardColors.red) {
        atMostTheseColors.OR.push({ colors_contains: `R` });
      }
      if (cardColors.green) {
        atMostTheseColors.OR.push({ colors_contains: `G` });
      }
    }
    where.AND.push(atMostTheseColors);
  }
};

export default addCardColorFilter;
