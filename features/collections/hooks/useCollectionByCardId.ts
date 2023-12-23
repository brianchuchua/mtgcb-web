import { useMemo } from 'react';
import { useGetCollectionByCardIdLegacyQuery } from '../../../network/services/mtgcbApi';

export const useCollectionByCardId = (userId: string, cardIds: number[], hasQuantityFilters: boolean) => {
  const {
    data: collectionByCardIdResponse,
    isLoading: isCollectionByCardIdLoading,
    isFetching: isCollectionByCardIdFetching,
    error: collectionByCardIdError,
  } = useGetCollectionByCardIdLegacyQuery(
    {
      userId,
      cardIds,
    },
    { skip: cardIds == null || hasQuantityFilters }
  );

  const collectionByCardId = useMemo(
    () =>
      collectionByCardIdResponse?.data?.collectionByCardIdLegacy?.collection?.reduce((acc, curr) => {
        acc[curr.cardID] = curr;
        return acc;
      }, {} as any),
    [collectionByCardIdResponse]
  ); // eslint-disable-line @typescript-eslint/no-explicit-any

  const collectionByCardIdWithDefaults = useMemo(
    () =>
      cardIds && collectionByCardId && !isCollectionByCardIdFetching
        ? cardIds?.reduce((acc, curr) => {
            if (!collectionByCardId[curr]) {
              acc[curr] = {
                cardID: Number(curr),
                quantityReg: 0,
                quantityFoil: 0,
              };
            } else {
              acc[curr] = collectionByCardId[curr];
            }
            return acc;
          }, {})
        : {},
    [collectionByCardId, cardIds, isCollectionByCardIdFetching]
  );

  return { collectionByCardIdWithDefaults, isCollectionByCardIdLoading, isCollectionByCardIdFetching, collectionByCardIdError };
};
