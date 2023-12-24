import { useGetAllSubsetsQuery } from '../../../network/services/mtgcbApi';

export const useGetSubsetsByParentSetId = (setId: string, isSubsetGroup: boolean) => {
  const { data: subsetData } = useGetAllSubsetsQuery(
    {
      parentSetId: setId,
    },
    {
      skip: !setId,
    }
  );

  const subsets = subsetData?.data?.sets;

  let goToOptions = [];
  if (subsets?.length > 0 && !isSubsetGroup) {
    goToOptions = subsets.map((subset) => ({
      label: subset.name,
      value: subset.slug,
    }));
  }

  return { subsets, goToOptions };
};
