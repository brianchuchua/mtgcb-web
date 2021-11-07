import { SetCategory } from '../../../../features/browse/browseSlice';

interface AddSetCategoryFilterFunction {
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  (setTypes: SetCategory[], where: { AND: any[] }): any;
}

const addSetCategoryFilter: AddSetCategoryFilterFunction = (setCategories, where) => {
  const setCategoryConditions = { AND: [], OR: [] };

  if (setCategories.length) {
    for (const setCategorySelection of setCategories) {
      const category = setCategorySelection.value;
      if (setCategorySelection.exclude) {
        setCategoryConditions.AND.push({ category_not: category });
      } else {
        setCategoryConditions.OR.push({ category });
      }
    }

    if (setCategoryConditions.OR.length === 0) {
      delete setCategoryConditions.OR;
    }
    where.AND.push(setCategoryConditions);
  }
};

export default addSetCategoryFilter;
