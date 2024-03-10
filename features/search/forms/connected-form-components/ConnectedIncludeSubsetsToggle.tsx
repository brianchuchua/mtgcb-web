import { useDispatch, useSelector } from 'react-redux';
import IncludeSubsetsToggle from '../../../../components/search-form-components/IncludeSubsetsToggle';
import { RootState } from '../../../../redux/rootReducer';
import { useQueryParameter } from '../../../../util/useQueryParameter';
import { ConnectedSearchFormComponentProps } from './types';

interface ConnectedIncludeSubsetsToggleProps extends ConnectedSearchFormComponentProps {
  setIncludeSubsets: any;
}

const ConnectedIncludeSubsetsToggle: React.FC<ConnectedIncludeSubsetsToggleProps> = ({ reduxSlice, setIncludeSubsets }) => {
  const dispatch = useDispatch();
  const { includeSubsets } = useSelector((state: RootState) => state[reduxSlice]);
  const setQueryParameter = useQueryParameter();

  const updateIncludeSubsets = (toggleValue) => {
    dispatch(setIncludeSubsets(toggleValue));

    if (reduxSlice === 'browse') {
      setQueryParameter('includeSubsetsBrowse', toggleValue ? '1' : '0');
    } else if (reduxSlice === 'collection') {
      setQueryParameter('includeSubsetsCollection', toggleValue ? '1' : '0');
    }
  };

  return <IncludeSubsetsToggle includeSubsets={includeSubsets} updateIncludeSubsets={updateIncludeSubsets} />;
};

export default ConnectedIncludeSubsetsToggle;
