import React, { memo } from 'react';
import { useSelector } from 'react-redux';
import { useGetFilteredCollectionSummaryLegacyQuery } from '../../network/services/mtgcbApi';
import { RootState } from '../../redux/rootReducer';
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

  // Tech debt: We should switch to a different query for this component since it can mess with prefetching of other components since it doesn't consume all of the same inputs
  const {
    data: collectionSummary,
    isLoading: isFilteredCollectionSummaryLoading,
    error: filteredCollectionSummaryError,
  } = useGetFilteredCollectionSummaryLegacyQuery({
    userId,
    priceType,
    first: expansionsFirst,
    skip: expansionsSkip,
    search: expansionSearchQuery,
    sortBy: sortExpansionBy,
    sortByDirection: sortExpansionByDirection,
    additionalSortBy: null,
    whereSetCompletionStatus: ['all'],
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

  return <MemoizedCollectionDetails collectionDetails={collectionDetails} isLoading={isFilteredCollectionSummaryLoading} />;
};

const MemoizedCollectionDetails = memo(CollectionDetails);
