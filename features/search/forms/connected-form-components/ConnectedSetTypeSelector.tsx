import isMobile from 'is-mobile';
import { useDispatch, useSelector } from 'react-redux';
import styled from 'styled-components';
import AutocompleteWithNegation from '../../../../components/AutocompleteWithNegation';
import NativeMultiselect from '../../../../components/NativeMultiselect';
import { useGetSetTypesQuery } from '../../../../network/services/mtgcbApi';
import { RootState } from '../../../../redux/rootReducer';
import { convertSetTypesToString } from '../../../../util/queryStringMappers';
import { useQueryParameter } from '../../../../util/useQueryParameter';
import { SetType } from '../../../browse/browseSlice';
import { ConnectedSearchFormComponentProps } from './types';

interface ConnectedSetTypeSelectorProps extends ConnectedSearchFormComponentProps {
  setExpansionTypes: any;
}

const ConnectedSetTypeSelector: React.FC<ConnectedSetTypeSelectorProps> = ({ reduxSlice, setExpansionTypes }) => {
  const dispatch = useDispatch();
  const setQueryParameter = useQueryParameter();

  const updateSetTypes = (newSetTypes: SetType[]) => {
    dispatch(setExpansionTypes({ setTypes: newSetTypes }));
    setQueryParameter('setTypes', convertSetTypesToString(newSetTypes));
  };

  const { expansionTypes } = useSelector((state: RootState) => state[reduxSlice]);

  const { data: setTypesResponse } = useGetSetTypesQuery();
  const setTypes = setTypesResponse?.data?.setTypes;
  const setTypesWithExclude = setTypes?.map((setType) => ({
    ...setType,
    exclude: false,
  }));

  const isMobileBrowser = isMobile();

  return setTypes?.length ? (
    <StyledSetTypeSelector>
      {!isMobileBrowser && (
        <AutocompleteWithNegation
          label="Set Types"
          options={setTypesWithExclude}
          selectedOptions={expansionTypes}
          setSelectedOptionsRemotely={updateSetTypes}
        />
      )}
      {isMobileBrowser && (
        <NativeMultiselect
          label="Set Types"
          multiselectOptions={setTypesWithExclude}
          selectedOptions={expansionTypes}
          updateSelectedOptions={updateSetTypes}
        />
      )}
    </StyledSetTypeSelector>
  ) : null;
};

const StyledSetTypeSelector = styled.div(() => ({
  paddingLeft: '8px',
  paddingRight: '8px',
  paddingBottom: '10px',
}));

export default ConnectedSetTypeSelector;
