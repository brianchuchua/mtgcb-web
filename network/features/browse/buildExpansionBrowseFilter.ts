import { SetCategory, SetType } from '../../../features/browse/browseSlice';
import { addSetCategoryFilter, addSetTypeFilter } from './filters';

interface BuildBrowseExpansionFilterSettings {
  name?: string;
  setTypes?: SetType[];
  setCategories?: SetCategory[];
  includeSubsets?: boolean;
  includeSubsetGroups?: boolean;
}

interface BuildBrowseExpansionFilterFunction {
  (filterSettings: BuildBrowseExpansionFilterSettings): any; // eslint-disable-line @typescript-eslint/no-explicit-any
}

const buildBrowseExpansionFilter: BuildBrowseExpansionFilterFunction = ({
  name,
  setTypes,
  setCategories,
  includeSubsets,
  includeSubsetGroups,
}) => {
  const where = { AND: [] };

  if (name) {
    where.AND.push({
      OR: [
        {
          name: {
            contains: name,
            mode: 'insensitive',
          },
        },
        {
          code: {
            equals: name,
            mode: 'insensitive',
          },
        },
      ],
    });
  }

  if (!includeSubsets) {
    where.AND.push({
      parentSetId: null,
    });
  }

  if (!includeSubsetGroups) {
    where.AND.push({
      isSubsetGroup: { equals: false },
    });
  }

  addSetTypeFilter(setTypes, where);
  addSetCategoryFilter(setCategories, where);

  return where;
};

export default buildBrowseExpansionFilter;
