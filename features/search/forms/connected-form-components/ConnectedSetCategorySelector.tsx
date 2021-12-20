import { useDispatch, useSelector } from 'react-redux';
import styled from 'styled-components';
import AutocompleteWithNegation from '../../../../components/AutocompleteWithNegation';
import { RootState } from '../../../../redux/rootReducer';
import { expansionCategoryOptions, SetCategory } from '../../../browse/browseSlice';
import { ConnectedSearchFormComponentProps } from './types';

interface ConnectedSetCategorySelectorProps extends ConnectedSearchFormComponentProps {
  setExpansionCategories: any;
}

const ConnectedSetCategorySelector: React.FC<ConnectedSetCategorySelectorProps> = ({ reduxSlice, setExpansionCategories }) => {
  const dispatch = useDispatch();

  const updateSetCategories = (selectedExpansionCategories: SetCategory[]) => {
    dispatch(setExpansionCategories({ setCategories: selectedExpansionCategories }));
  };

  const { expansionCategories } = useSelector((state: RootState) => state[reduxSlice]);

  return (
    <StyledSetCategorySelector>
      <AutocompleteWithNegation
        label="Set Categories"
        options={expansionCategoryOptions}
        selectedOptions={expansionCategories}
        setSelectedOptionsRemotely={updateSetCategories}
      />
    </StyledSetCategorySelector>
  );
};

const StyledSetCategorySelector = styled.div(() => ({
  paddingLeft: '8px',
  paddingRight: '8px',
  paddingBottom: '10px',
}));

export default ConnectedSetCategorySelector;
