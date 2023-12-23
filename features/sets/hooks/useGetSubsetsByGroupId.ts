import { useGetAllSubsetsWithGroupIdQuery } from '../../../network/services/mtgcbApi';

export const useGetSubsetsByGroupId = (setId: string) => {
  const {
    data: subsetByGroupIdData,
    isLoading: isSubsetByGroupIdLoading,
    isFetching: isSubsetByGroupIdFetching,
    error: subsetByGroupIdError,
  } = useGetAllSubsetsWithGroupIdQuery(
    {
      subsetGroupId: setId,
    },
    {
      skip: !setId,
    }
  );

  const subsetsByGroupId = subsetByGroupIdData?.data?.sets;

  let subsetByGroupIdOptions = [{ label: 'All', value: 'All' }];
  let unsortedSubsets = [];
  if (subsetsByGroupId?.length > 0) {
    unsortedSubsets = unsortedSubsets.concat(
      subsetsByGroupId.map((subset) => ({
        label: subset.name,
        value: subset.id,
      }))
    );
  }
  subsetByGroupIdOptions = subsetByGroupIdOptions.concat(unsortedSubsets.sort((a, b) => a.label.localeCompare(b.label)));

  return { subsetByGroupIdOptions, isSubsetByGroupIdLoading, isSubsetByGroupIdFetching, subsetByGroupIdError };
};
