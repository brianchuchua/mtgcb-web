import isMobile from 'is-mobile';
import { useDispatch, useSelector } from 'react-redux';
import styled from 'styled-components';
import AutocompleteWithNegation from '../../../../components/AutocompleteWithNegation';
import NativeMultiselect from '../../../../components/NativeMultiselect';
import { useGetAllSetNamesQuery } from '../../../../network/services/mtgcbApi';
import { RootState } from '../../../../redux/rootReducer';
import { convertSetsToString } from '../../../../util/queryStringMappers';
import { useQueryParameter } from '../../../../util/useQueryParameter';
import { CardSet } from '../../../browse/browseSlice';
import { mapCardSets } from '../../../browse/forms/mappers';
import { ConnectedSearchFormComponentProps } from './types';

interface ConnectedSetSelectorProps extends ConnectedSearchFormComponentProps {
  setCardSets: any;
}

const ConnectedSetSelector: React.FC<ConnectedSetSelectorProps> = ({ reduxSlice, setCardSets }) => {
  const dispatch = useDispatch();
  const setQueryParameter = useQueryParameter();

  const updateSets = (newSets: CardSet[]) => {
    dispatch(setCardSets({ cardSets: newSets }));
    setQueryParameter('sets', convertSetsToString(newSets));
  };

  const { cardSets } = useSelector((state: RootState) => state[reduxSlice]);

  const { data: allSetNamesResponse } = useGetAllSetNamesQuery({});
  const allSetNames = allSetNamesResponse?.data?.sets;
  const sets = mapCardSets(allSetNames || []);

  const isMobileBrowser = isMobile();

  return (
    sets && (
      <StyledSetSelector>
        {!isMobileBrowser && (
          <AutocompleteWithNegation label="Card Sets" options={sets} selectedOptions={cardSets} setSelectedOptionsRemotely={updateSets} />
        )}
        {isMobileBrowser && (
          <NativeMultiselect label="Card Sets" multiselectOptions={sets} selectedOptions={cardSets} updateSelectedOptions={updateSets} />
        )}
      </StyledSetSelector>
    )
  );
};

const StyledSetSelector = styled.div(() => ({
  paddingLeft: '8px',
  paddingRight: '8px',
  paddingBottom: '10px',
}));

export default ConnectedSetSelector;
