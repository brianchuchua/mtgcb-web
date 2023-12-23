import { useEffect } from 'react';
import { useSelector } from 'react-redux';
import { useGetAllSetsMetaQuery, useGetAllSetsQuery } from '../../../network/services/mtgcbApi';
import { RootState } from '../../../redux/rootReducer';
import { usePagination } from '../../../util/usePagination';

export const useExpansionSearch = () => {
  const {
    expansionSearchQuery,
    sortExpansionBy,
    sortExpansionByDirection,
    expansionTypes,
    expansionCategories,
    includeSubsets,
    includeSubsetGroups,
  } = useSelector((state: RootState) => state.browse);

  const { skip, setSkip, first, setFirst, page, setPage, handleTotalResultsChange } = usePagination({
    initialPage: 1,
    initialPageSize: 20,
    localStorageKey: 'numberOfExpansionsPerPage',
  });

  const { data: allSetsResponse, isLoading: isGetAllSetsLoading, isFetching: isGetAllSetsFetching } = useGetAllSetsQuery({
    first,
    skip,
    name: expansionSearchQuery,
    sortBy: sortExpansionBy,
    sortByDirection: sortExpansionByDirection,
    setTypes: expansionTypes,
    setCategories: expansionCategories,
    includeSubsets,
    includeSubsetGroups,
  });

  const expansions = allSetsResponse?.data?.sets;

  const { data: allSetsMetaResponse, isLoading: isAllSetsMetaLoading, isFetching: isAllSetsMetaFetching } = useGetAllSetsMetaQuery({
    name: expansionSearchQuery,
    sortBy: sortExpansionBy,
    sortByDirection: sortExpansionByDirection,
    setTypes: expansionTypes,
    setCategories: expansionCategories,
    includeSubsets,
    includeSubsetGroups,
  });

  const totalExpansionsResults = allSetsMetaResponse?.data?.count || 0;

  useEffect(() => {
    handleTotalResultsChange(totalExpansionsResults);
  }, [skip, totalExpansionsResults]);

  return {
    expansions,
    isGetAllSetsLoading,
    isGetAllSetsFetching,
    isAllSetsMetaLoading,
    isAllSetsMetaFetching,
    expansionsSkip: skip,
    setExpansionsSkip: setSkip,
    expansionsFirst: first,
    setExpansionsFirst: setFirst,
    expansionsPage: page,
    setExpansionsPage: setPage,
    totalExpansionsResults,
  };
};
