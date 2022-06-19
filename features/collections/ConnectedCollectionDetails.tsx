import React, { memo } from 'react';
import { useSelector } from 'react-redux';
import { useGetFilteredCollectionSummaryLegacyQuery } from '../../network/services/mtgcbApi';
import { RootState } from '../../redux/rootReducer';
import useDebounce, { searchFieldDebounceTimeMs } from '../../util/useDebounce';
import { CollectionDetails } from './CollectionDetails';

interface ConnectedCollectionDetailsProps {
  userId: string;
  expansionsFirst: number;
  expansionsSkip: number;
}

export const ConnectedCollectionDetails: React.FC<ConnectedCollectionDetailsProps> = ({ userId, expansionsFirst, expansionsSkip }) => {
  const { sortExpansionBy, sortExpansionByDirection, priceType, expansionTypes, expansionCategories, expansionSearchQuery } = useSelector(
    (state: RootState) => state.collection
  );

  const debouncedExpansionSearchQuery = useDebounce(expansionSearchQuery, searchFieldDebounceTimeMs);

  const {
    data: collectionSummary,
    isLoading: isFilteredCollectionSummaryLoading,
    error: filteredCollectionSummaryError,
  } = useGetFilteredCollectionSummaryLegacyQuery({
    userId,
    priceType,
    first: expansionsFirst,
    skip: expansionsSkip,
    search: debouncedExpansionSearchQuery,
    sortBy: sortExpansionBy,
    sortByDirection: sortExpansionByDirection,
    additionalSortBy: null,
    whereSetCompletionStatus: null,
    setTypes: expansionTypes,
    setCategories: expansionCategories,
  });

  const collectionDetails = {
    username: collectionSummary?.data?.filteredCollectionSummaryLegacy?.username,
    numberOfCardsInMagic: collectionSummary?.data?.filteredCollectionSummaryLegacy?.numberOfCardsInMagic,
    totalCardsCollected: collectionSummary?.data?.filteredCollectionSummaryLegacy?.totalCardsCollected,
    uniquePrintingsCollected: collectionSummary?.data?.filteredCollectionSummaryLegacy?.uniquePrintingsCollected,
    percentageCollected: collectionSummary?.data?.filteredCollectionSummaryLegacy?.percentageCollected,
    totalValue: collectionSummary?.data?.filteredCollectionSummaryLegacy?.totalValue?.[priceType],
  };

  return <MemoizedCollectionDetails collectionDetails={collectionDetails} />;
};

const MemoizedCollectionDetails = memo(CollectionDetails);
