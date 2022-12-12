import { SetType } from '../../../../features/browse/browseSlice';

interface AddSetTypeFilterFunction {
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  (setTypes: SetType[], where: { AND: any[] }): any;
}

const addSetTypeFilter: AddSetTypeFilterFunction = (setTypes, where) => {
  const setTypeConditions = { AND: [], OR: [] };

  if (setTypes.length) {
    for (const setTypeSelection of setTypes) {
      const setType = setTypeSelection.value;
      if (setTypeSelection.exclude) {
        setTypeConditions.AND.push({ setType: { not: setType } });
      } else {
        setTypeConditions.OR.push({ setType: { equals: setType } });
      }
    }

    if (setTypeConditions.OR.length === 0) {
      delete setTypeConditions.OR;
    }
    where.AND.push(setTypeConditions);
  }
};

export default addSetTypeFilter;
