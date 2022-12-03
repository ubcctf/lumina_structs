#convenience method for construct readable dicts
class AttrDict(dict):
    def __getattr__(self, __key):
        return self[__key]  
        #this class is for building constructs from dicts back into bytes, so return empty byte on no found by default
        #it might make things harder to debug though