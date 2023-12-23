import { useCallback, useEffect } from 'react';
import { useSelector } from 'react-redux';
import { useGetFilteredCollectionSummaryLegacyQuery, usePrefetch } from '../../../network/services/mtgcbApi';
import { RootState } from '../../../redux/rootReducer';

export const useFilteredCollectionSummary = (userId: string, reduxSlice: string, expansionsFirst: number, expansionsSkip: number) => {
  const {
    sortExpansionBy,
    sortExpansionByDirection,
    priceType,
    expansionTypes,
    expansionCategories,
    expansionSearchQuery,
    setCompletionStatuses,
    includeSubsets,
    includeSubsetGroups,
    includeSubsetsInSets,
  } = useSelector((state: RootState) => state[reduxSlice]);

  const {
    data: collectionSummary,
    isLoading: isFilteredCollectionSummaryLoading,
    isFetching: isFilteredCollectionSummaryFetching,
    error: filteredCollectionSummaryError,
  } = useGetFilteredCollectionSummaryLegacyQuery({
    userId,
    priceType,
    first: expansionsFirst,
    skip: expansionsSkip,
    search: expansionSearchQuery,
    sortBy: sortExpansionBy,
    sortByDirection: sortExpansionByDirection,
    additionalSortBy: null, // TODO: Check me -- should I be null?
    whereSetCompletionStatus: setCompletionStatuses ?? ['all'],
    setTypes: expansionTypes,
    setCategories: expansionCategories,
    includeSubsets,
    includeSubsetGroups,
    includeSubsetsInSets,
  });

  const expansions = collectionSummary?.data?.filteredCollectionSummaryLegacy?.collectionSummary;
  const costsToPurchase = collectionSummary?.data?.filteredCollectionSummaryLegacy?.collectionSummary;
  const totalExpansionsResults = collectionSummary?.data?.filteredCollectionSummaryLegacy?.count ?? 0;

  const prefetchPage = usePrefetch('getFilteredCollectionSummaryLegacy');

  const prefetchNext = useCallback(() => {
    if (expansionsSkip + expansionsFirst < totalExpansionsResults) {
      prefetchPage({
        userId,
        priceType,
        first: expansionsFirst,
        skip: expansionsSkip + expansionsFirst,
        search: expansionSearchQuery,
        sortBy: sortExpansionBy,
        sortByDirection: sortExpansionByDirection,
        additionalSortBy: null,
        whereSetCompletionStatus: setCompletionStatuses ?? ['all'],
        setTypes: expansionTypes,
        setCategories: expansionCategories,
        includeSubsets,
        includeSubsetGroups,
        includeSubsetsInSets,
      });
    }
  }, [
    userId,
    priceType,
    expansionsFirst,
    expansionsSkip,
    expansionSearchQuery,
    sortExpansionBy,
    sortExpansionByDirection,
    setCompletionStatuses,
    expansionTypes,
    expansionCategories,
    totalExpansionsResults,
  ]);

  useEffect(() => {
    if (expansionsSkip + expansionsFirst < totalExpansionsResults) {
      prefetchNext();
    }
  }, [expansionsSkip, expansionsFirst, totalExpansionsResults, prefetchNext]);

  return {
    expansions,
    costsToPurchase,
    totalExpansionsResults,
    isFilteredCollectionSummaryLoading,
    isFilteredCollectionSummaryFetching,
    prefetchNext,
  };
};
