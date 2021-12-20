import { useDispatch, useSelector } from 'react-redux';
import styled from 'styled-components';
import AutocompleteWithNegation from '../../../../components/AutocompleteWithNegation';
import { useGetSetTypesQuery } from '../../../../network/services/mtgcbApi';
import { RootState } from '../../../../redux/rootReducer';
import { SetType } from '../../../browse/browseSlice';
import { ConnectedSearchFormComponentProps } from './types';

interface ConnectedSetTypeSelectorProps extends ConnectedSearchFormComponentProps {
  setExpansionTypes: any;
}

const ConnectedSetTypeSelector: React.FC<ConnectedSetTypeSelectorProps> = ({ reduxSlice, setExpansionTypes }) => {
  const dispatch = useDispatch();

  const updateSetTypes = (newSetTypes: SetType[]) => {
    dispatch(setExpansionTypes({ setTypes: newSetTypes }));
  };

  const { expansionTypes } = useSelector((state: RootState) => state[reduxSlice]);

  const { data: setTypesResponse } = useGetSetTypesQuery();
  const setTypes = setTypesResponse?.data?.setTypes;
  const setTypesWithExclude = setTypes?.map((setType) => ({
    ...setType,
    exclude: false,
  }));

  return setTypes?.length ? (
    <StyledSetTypeSelector>
      <AutocompleteWithNegation
        label="Set Types"
        options={setTypesWithExclude}
        selectedOptions={expansionTypes}
        setSelectedOptionsRemotely={updateSetTypes}
      />
    </StyledSetTypeSelector>
  ) : null;
};

const StyledSetTypeSelector = styled.div(() => ({
  paddingLeft: '8px',
  paddingRight: '8px',
  paddingBottom: '10px',
}));

export default ConnectedSetTypeSelector;
