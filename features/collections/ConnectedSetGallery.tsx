import React, { memo, useCallback, useEffect } from 'react';
import { useSelector } from 'react-redux';
import { useGetFilteredCollectionSummaryLegacyQuery, usePrefetch } from '../../network/services/mtgcbApi';
import { RootState } from '../../redux/rootReducer';
import SetGallery from '../browse/SetGallery';

interface ConnectedSetGalleryProps {
  userId: string;
  expansionsFirst: number;
  expansionsSkip: number;
  expansionsPage: number;
  setExpansionsSkip: (skip: number) => void;
  setExpansionsFirst: (first: number) => void;
  setExpansionsPage: (page: number) => void;
}

export const ConnectedSetGallery: React.FC<ConnectedSetGalleryProps> = ({
  userId,
  expansionsFirst,
  expansionsSkip,
  expansionsPage,
  setExpansionsSkip,
  setExpansionsFirst,
  setExpansionsPage,
}) => {
  const {
    sortExpansionBy,
    sortExpansionByDirection,
    priceType,
    expansionTypes,
    expansionCategories,
    expansionSearchQuery,
    setCompletionStatuses,
  } = useSelector((state: RootState) => state.collection);

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

  return (
    <MemoizedSetGallery
      sets={expansions}
      costsToPurchase={costsToPurchase}
      totalResults={totalExpansionsResults}
      first={expansionsFirst}
      skip={expansionsSkip}
      page={expansionsPage}
      setSkip={setExpansionsSkip}
      setFirst={setExpansionsFirst}
      setPage={setExpansionsPage}
      priceType={priceType}
      userId={userId}
      isLoading={isFilteredCollectionSummaryLoading}
      isFetching={isFilteredCollectionSummaryFetching}
    />
  );
};

const MemoizedSetGallery = memo(SetGallery);
