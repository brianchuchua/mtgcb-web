import { useDispatch, useSelector } from 'react-redux';
import IncludeSubsetGroupsToggle from '../../../../components/search-form-components/IncludeSubsetGroupsToggle';
import { RootState } from '../../../../redux/rootReducer';
import { useQueryParameter } from '../../../../util/useQueryParameter';
import { ConnectedSearchFormComponentProps } from './types';

interface ConnectedIncludeSubsetGroupsToggleProps extends ConnectedSearchFormComponentProps {
  setIncludeSubsetGroups: any;
}

const ConnectedIncludeSubsetGroupsToggle: React.FC<ConnectedIncludeSubsetGroupsToggleProps> = ({ reduxSlice, setIncludeSubsetGroups }) => {
  const dispatch = useDispatch();
  const { includeSubsetGroups } = useSelector((state: RootState) => state[reduxSlice]);
  const setQueryParameter = useQueryParameter();

  const updateIncludeSubsetGroups = (toggleValue) => {
    dispatch(setIncludeSubsetGroups(toggleValue));
    if (reduxSlice === 'browse') {
      setQueryParameter('includeSubsetGroupsBrowse', toggleValue ? '1' : '0');
    } else if (reduxSlice === 'collection') {
      setQueryParameter('includeSubsetGroupsCollection', toggleValue ? '1' : '0');
    }
  };

  return <IncludeSubsetGroupsToggle includeSubsetGroups={includeSubsetGroups} updateIncludeSubsetGroups={updateIncludeSubsetGroups} />;
};

export default ConnectedIncludeSubsetGroupsToggle;
